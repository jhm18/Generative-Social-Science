// haar.stan
// Joris Chau
// 16 June 2021

// Filter C coefficient vector
vector filtC(vector C, int N) {
    vector[N] C1;
    for (n in 1 : N) {
        C1[n] = (C[2 * n - 1] + C[2 * n]) / sqrt2();
    }
    return C1;
}

// Filter D coefficient vector
vector filtD(vector D, int N) {
    vector[N] D1;
    for (n in 1 : N) {
        D1[n] = (D[2 * n - 1] - D[2 * n]) / sqrt2();
    }
    return D1;
}

// Reconstruct C coefficient vector
vector inv_filt(vector C, vector D, int N) {
    vector[2 * N] C1;
    for (n in 1 : N) {
        C1[2 * n - 1] = (C[n] + D[n]) / sqrt2();
        C1[2 * n] = (C[n] - D[n]) / sqrt2();
    }
    return C1;
}

// Forward Haar wavelet transform
vector fwt(vector y) {
    int N = rows(y);
    int Ni = 0;
    vector[N] ywd;
    vector[N] C = y;
    while (N > 1) {
        N /= 2;
        ywd[(Ni + 1):(Ni + N)] = filtD(C[1 : (2 * N)], N);
        C[1:N] = filtC(C[1 : (2 * N)], N);
        Ni += N;
    }
    ywd[Ni + 1] = C[1];
    return ywd;
}

// Inverse Haar wavelet transform
vector iwt(vector ywd) {
    int N = rows(ywd);
    vector[N] y;
    int Nj = 1;
    y[1] = ywd[N];
    while (Nj < N) {
        y [1 : (2 * Nj)] = inv_filt(y[1 : Nj], ywd[(N - 2 * Nj + 1) : (N - Nj)], Nj);
        Nj *= 2;
    }
    return y;
}