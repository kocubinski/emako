#/bin/bash

set -o errexit

GOLANG_PROTOBUF_VERSION=1.28.1
GRPC_GATEWAY_VERSION=1.16.0
SRCDIR=$HOME/dev

go install github.com/cosmos/cosmos-proto/cmd/protoc-gen-go-pulsar@latest 
go install google.golang.org/protobuf/cmd/protoc-gen-go@v${GOLANG_PROTOBUF_VERSION} 
go install github.com/grpc-ecosystem/grpc-gateway/protoc-gen-grpc-gateway@v${GRPC_GATEWAY_VERSION} \
      github.com/grpc-ecosystem/grpc-gateway/protoc-gen-swagger@v${GRPC_GATEWAY_VERSION}

# install all gogo protobuf binaries
cd $SRCDIR
git clone -b v1.4.3 https://github.com/cosmos/gogoproto.git; \
  cd gogoproto; \
  go mod download; \
  make install
