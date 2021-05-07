echo "api:" $(kubectl -n default get endpoints kubernetes -ojsonpath='https://{@.subsets[0].addresses[0].ip}:{@.subsets[0].ports[0].port}')

secretName=$(kubectl get sa report-reader -n kube-system -o jsonpath='{.secrets[0].name}')
echo "token :" $(kubectl get secret $secretName -n kube-system -o jsonpath={.data.token} | base64 -d)
