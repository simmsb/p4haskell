# p4haskell

A GPU-accelerated back end for the PR programming language.

# Usage

1. The first step is to compile a p4 program with the GPU back end present in my
   fork of the p4c compiler[^1], and dump the AST, for example:
   `./build/backends/gpu/p4c-gpu port-forwarding.p4 --toJSON port-forward.json`
2. The next step is to use this compiler to generate a CUDA file, for example:
   `stack run -- -i firewall.json > kernel.cu`
3. The final step is to place this CUDA file into the runtime[^2] as
   `src/kernel.cu`, and compile the runtime using `cargo build --release`. The
   runtime can then be executed using `sudo env
   LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/usr/local/lib"
   target/release/dpdk-gpu-test -s payload_1024.pcap`

[^1]: https://github.com/simmsb/p4c

[^2]: https://github.com/simmsb/dpdk-testing-stuff
