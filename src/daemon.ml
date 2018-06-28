open LidcoreBsNode

external __dirname : string = "" [@@bs.val]

let instancesDir =
  Fs.realpathSync {j|$(__dirname)/../../../src/instances|j}

let () =
  if Fs.existsSync instancesDir then
    RequireAll.exec instancesDir;
  Instances.Runtime.run ()

