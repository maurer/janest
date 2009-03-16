#!/bin/bash -e
if [ Darwin = $(uname -s) ]; then
  PP="cc -E -no-cpp-precomp -x c"
else
  PP="cpp"
fi
source_file=${1}; shift
${PP} ${source_file} | grep --after-context=100000 'end-pp-include' | grep -v "end-pp-include"
#if expr match ${source_file} '.ml$'; then
#else
#  ${PP} ${source_file}
#fi
