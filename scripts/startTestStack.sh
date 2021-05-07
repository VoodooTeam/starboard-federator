#!/usr/bin/env bash

kind delete cluster --name starboard || true
kind create cluster --name starboard

helm repo add aqua https://aquasecurity.github.io/helm-charts/ && helm repo update
helm install starboard-operator aqua/starboard-operator \
	-n starboard-operator --create-namespace \
	--set="targetNamespaces=" \
	--version 0.5.1

echo "Creating test resources..."
kubectl create ns test1
kubectl create ns test2
kubectl create deploy secu-test --image=nginx -n test1
kubectl create deploy secu-test --image=redis -n test2

echo "Waiting for reports to be available..."
while [ "$(kubectl get configauditreports --all-namespaces -ojson | jq '.items == []')" == "true" ]
do 
  echo -n "."
  sleep 1
done

echo
echo "Reports available !"
