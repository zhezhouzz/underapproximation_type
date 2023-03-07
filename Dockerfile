FROM ocaml/opam:debian-ocaml-4.12
RUN sudo apt-get update
RUN sudo apt-get install -y python3 python3-pip
RUN pip3 install tabulate
RUN sudo apt-get install -y libgmp-dev
RUN opam init --auto-setup
RUN opam install coq.8.14.1
RUN opam repo add coq-released https://coq.inria.fr/opam/released
RUN opam install coq-stdpp.1.7.0
RUN opam install dune.3.1.0
RUN opam install core.v0.15.0
RUN opam install core_unix.v0.15.0
RUN opam install yojson.1.7.0
RUN opam install conf-c++.1.0
RUN opam install conf-python-3.1.0.0
RUN opam install qcheck.0.18.1
RUN opam install merlin.4.5-412
RUN opam install ocolor.1.3.0
RUN opam install dolog.6.0.0
RUN opam install menhirLib.20220210
RUN opam install z3.4.8.14
RUN opam install menhir.20220210
RUN opam install ocamlbuild.0.14.1
RUN sudo apt-get install -y vim
SHELL ["/bin/bash", "-lc"]
ARG CACHEBUST=1
RUN git clone https://github.com/zhezhouzz/ocaml_parser.git && cd ocaml_parser && git pull origin ce0c760b2c6df0186064ad1093b492ac3686835a && git checkout ce0c760b2c6df0186064ad1093b492ac3686835a && opam install . && cd ..
RUN git clone https://github.com/zhezhouzz/zzdatatype.git && cd zzdatatype && git pull origin 55f9d6b5a4c483ac2397fd33740f1a4dc86a442a && git checkout 55f9d6b5a4c483ac2397fd33740f1a4dc86a442a && opam install . && cd ..
RUN git clone https://github.com/zhezhouzz/utils.git && cd utils && git pull origin ad8d11b955469fdb32240bf8131aceb944b3b31d && git checkout ad8d11b955469fdb32240bf8131aceb944b3b31d && opam install . && cd ..
RUN eval $(opam env)
RUN git clone https://github.com/zhezhouzz/normalty.git && cd normalty && git pull origin 83d9ac7f39c43b2e0335eba01e89c5c6a4648a1d && git checkout 83d9ac7f39c43b2e0335eba01e89c5c6a4648a1d && opam install . && cd ..
RUN eval $(opam env)
RUN git clone https://github.com/aegis-iisc/propsynth.git && cd propsynth && git pull origin 0d03e2f966d63a33a71c22d8d20bff9c37e721fc && git checkout 0d03e2f966d63a33a71c22d8d20bff9c37e721fc && cd ..
RUN git clone https://github.com/zhezhouzz/underapproximation_type.git
WORKDIR underapproximation_type
RUN git config pull.ff only
RUN git checkout artifact
RUN git pull origin 3f69a57b3f6b9e533f9e63355ca55d9bf32e4d92
RUN git checkout -b 3f69a57b3f6b9e533f9e63355ca55d9bf32e4d92