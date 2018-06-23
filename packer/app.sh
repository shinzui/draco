#!/bin/sh -e

# OCaml
eval `opam config env`

# Systemd
systemd_target="/lib/systemd/system/draco.service"
base_dir="${HOME}/draco"
node_binary=`which node`
run_script="${base_dir}/src/daemon.js"

cd ~/draco
cat /tmp/draco.systemd.in | \
    sed -e "s#@base_dir@#${base_dir}#g" | \
    sed -e "s#@node_binary@#${node_binary}#g" | \
    sed -e "s#@run_script@#${run_script}#g" >| packer/draco.systemd

sudo cp -f packer/draco.systemd "${systemd_target}"
sudo systemctl enable draco.service
