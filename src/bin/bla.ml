open BsAsyncMonad.Callback

let projectId = "komodo-staging-99c51"
let zone = "us-east1-b"
let name = "foo"
let compute =
  Gcloud.Compute.init ~config:(Gcloud.config ~projectId ()) ()
let zone =
  Gcloud.Compute.zone compute zone
let instanceGroupManager =
  Gcloud.Compute.Zone.instanceGroupManager zone name
let () =
  finish (Gcloud.Compute.Zone.InstanceGroupManager.delete instanceGroupManager)
