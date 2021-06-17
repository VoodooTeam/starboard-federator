# Starboard federator
Aggregates [Aqua Starboard](https://blog.aquasec.com/starboard-kubernetes-tools) reports from several clusters

## Start dev server
IHP relies on `nix` package manager, you must install it first : [(see IHP doc)](https://ihp.digitallyinduced.com/Guide/installation.html#1-dependency-nix-package-manager)

```
./start
```
should be enough ;)

## dev env
start a target cluster (with starboard operator installed and the CR, CRB & serviceAccount for the federator)
```
make startTestStack
```
this will output the URL and serviceAccount token so you can the federator can aggregate results

you can open a shell with all needed softwares by running 
```
nix-shell ./dev-shell.nix
```

## test deployment setup
start an "infra" cluster where you can test the federator deployment (not needed if you work on the app's code locally)
```
make local-env
```
this will also run a local docker regitstry available on port 5000


build & deploy the Staboard federator :
```
make build-deploy-local
```

### Creating the db schema
2 files are necessary (apply order matters) :
- ./build/ihp-lib/IHPSchema.sql
- ./Application/Schema.sql

### k8s deployment
a working (simple) example is available [here](./scripts/deploy/)
