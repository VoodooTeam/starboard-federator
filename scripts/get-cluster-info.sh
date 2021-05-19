echo "api:" $(kubectl config view --minify -o jsonpath='{.clusters[0].cluster.server}')

secretName=$(kubectl get sa starboard-report-reader -n kube-system -o jsonpath='{.secrets[0].name}')
echo "token :" $(kubectl get secret $secretName -n kube-system -o jsonpath={.data.token} | base64 -d)
