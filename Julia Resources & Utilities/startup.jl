ENV["EDITOR"] = "atom"

using OhMyREPL

#Keep Modes Sticky
import REPL
Base.atreplinit() do repl
    # make the ; shell mode sticky
    !isdefined(repl, :interface) && (repl.interface = REPL.setup_interface(repl))
    repl.interface.modes[2].sticky=true
end

try
    @eval using Revise
    # Turn on Revise's automatic-evaluation behavior
    Revise.async_steal_repl_backend()
catch err
    @warn "Could not load Revise."
end

# Update all packages, but do so in a worker process
import Distributed
let
    pkg_worker = Distributed.addprocs(1)[end]
    Distributed.remotecall(pkg_worker) do
        redirect_stdout() # silence everything, only on this worker
        Pkg.update()

        # now remove this worker and say we are done
        remotecall(1) do
            eval(quote
                Distributed.rmprocs($(pkg_worker))
                printstyled("\n Pkg.update() complete \n"; color=:light_black)
            end)
        end
    end
end