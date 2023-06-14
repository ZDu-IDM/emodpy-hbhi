# CI/CD Deployments  

## Prerequisites  
- Azure subscription with permissions to access "RSE Applications/rseuxstage-rg" resource group.  
- K8s cluster has been initialized for RSE web app deployments (for more info ask the k8s cluster admin).     

## Approach  
This solution is based in GitHub actions and K8s GitHub runners.    
- The runner handles authentication, image building and K8s deployment.   
- The GitHub action [deploy-stage.yml](../.github/workflows/deploy-stage.yml) supplies the runner with the code and triggers the CI/CD process.  
- The app defines its release process in [skaffold.yaml](skaffold.yaml) which is then executed in the runner by Skaffold tool.
- The GitHub action [promote-to-prod.yml](../.github/workflows/promote-to-prod.yml), on demand, deploys specified git tag to the production cluster. 

## Deploying App  
See GitHub action [deploy-stage.yml](../.github/workflows/deploy-stage.yml)    

## Promote Images
Context: After merging PR into the main branch, CD automation is deploying the new version to the rseuxstage cluster.   
The client/services images promotion process looks like this:  
- Open terminal (from local env. or GH runner), position to git repo, switch kubectl context to rseuxstage  
- Run promote-tag.sh to create the promotion git tag   
  (the script will parse image names from the deployment and set them as tag message - you can do that manually too)   
```bash
# Create new tag (omit 3rd argument to only see the command without executing) 
./promote-tag.sh main v1.01 Yes
```
- Push the generated tag to the origin (this will trigger the GA to promote tagged version to Prod)
```bash
git push origin v1.01  
```
- Run promote-to-prod GitHub action, specify the tag name and confirmation "Yes".  

See GitHub action [promote-to-prod.yml](../.github/workflows/promote-to-prod.yml)

## GitHub Runner
Setting up the github runner is done by the cluster admin and neededs to happen only at the start of the project.

### GitHub Runner Tools  
*(install these locally only if you plan to interact with the k8s cluster, test the deployment process or seal secrets)*    
These are the tools used in the deployment process. They are already installed in the GitHub runner docker image (except SealedSecrets).   
- [Azure CLI](https://docs.microsoft.com/en-us/cli/azure/install-azure-cli) (if you haven't already)  
- [kubectl](https://kubernetes.io/docs/tasks/tools/install-kubectl/), install by running ```az aks install-cli```  
- [Skaffold](https://skaffold.dev/docs/install/) v1.20.0 - builds, tags and pushes images and deploys the app to K8s   
- [SealedSecrets](https://github.com/bitnami-labs/sealed-secrets/releases) - K8s controller, encrypted secrets, allows keeping them in GitHub  

### Deploying Runner(s)  
This is needed only for the initial CI/CD setup (the first time) or if the number of GitHub runners has to be changed.   

```bash
kubectl apply -f ./github-runner/deployment.yaml  
``` 

