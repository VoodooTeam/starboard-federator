# Starboard federator
Aggregate Starboard reports from several clusters

## Start dev server
IHP relies on `nix` package manager, you must install it first : [(see IHP doc)](https://ihp.digitallyinduced.com/Guide/installation.html#1-dependency-nix-package-manager)

```
./start
```

## dev env
start a target cluster (with starboard operator installed and the CR, CRB & serviceAccount for the federator)
```
make startTestStack
```
this will output the URL and serviceAccount token so you can the federator can aggregate results

start an "infra" cluster where you can test the federator deployment (not needed if you work on the app's code locally)
```
make startInfraStack
```
this will also run a local docker regitstry available on port 5000 


build & deploy the Staboard federator :
```
docker build -t localhost:5000/starboard-federator .
docker push localhost:5000/starboard-federator
kubectl apply -f ./scripts/deploy/
```

## Get reports on target clusters using Curl
### Before all 
create the serviceAccount on target clusters (run manually by an admin)
```
kubectl apply -f ./scripts/create-sa.yml
```

then (will be provided to the app as Cluster entity)
```
export K8S_API=$(kubectl -n default get endpoints kubernetes -ojsonpath='https://{@.subsets[0].addresses[0].ip}:{@.subsets[0].ports[0].port}')
secretName=$(kubectl get sa report-reader -n kube-system -o jsonpath='{.secrets[0].name}')
export SA_TOKEN=$(kubectl get secret $secretName -n kube-system -o jsonpath={.data.token} | base64 -d)
```

### ConfigAuditReports

```
curl -s $K8S_API/apis/aquasecurity.github.io/v1alpha1/configauditreports  --header "Authorization: Bearer $SA_TOKEN" -k | jq
```

### VulnerabilityReports
```
curl -s $K8S_API/apis/aquasecurity.github.io/v1alpha1/vulnerabilityreports  --header "Authorization: Bearer $SA_TOKEN" -k | jq
```

NB: the `-k` or custom `ca.crt` shouldn't be necessary when communicating with EKS clusters

## run poll cluster job
mechanism : another simple k8s cronjob (postgresql image should do the job) inserts an empty record in the `poll_clusters_job` table

trigger the job during dev :
- open the psql REPL with `make psql`
- run `INSERT INTO poll_clusters_job DEFAULT VALUES;`

todo : add k8s cronjob to do that regulary.

## Creating the db schema
2 files are necessary (apply order matters) :
- ./build/ihp-lib/IHPSchema.sql
- ./Application/Schema.sql

## k8s deployment
a working (simple) example is available [here](./scripts/deploy/)
