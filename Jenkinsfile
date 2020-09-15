pipeline {
  agent {
    label 'nix'
  }

  stages {
    stage('Build and Test') {
      steps {
        sh 'nix-build -A euler-events-hs --option sandbox false --arg remoteDeps true'
        sh 'nix-shell --run "cabal v2-test"'
      }
    }
    stage('Summary') {
      steps {
        sh '''cat << EOF > ses_destination.json
{
  "ToAddresses":  ["sre-team@juspay.in"],
  "CcAddresses":  ["infra@juspay.in"],
  "BccAddresses": []
}
EOF
cat << EOF > ses_message.json
{
   "Subject": {
       "Data": "Jenkins: ${JOB_NAME} / Build: (${BUILD_NUMBER}) / Completed",
       "Charset": "UTF-8"
   },
   "Body": {
       "Html": {
           "Data": "Build - <a href=\\"${BUILD_URL}\\" target=\\"_blank\\">${JOB_NAME}/#${BUILD_NUMBER}</a> is completed successfully.<br><hr><br>Git Commit: ${GIT_COMMIT}<br>Git branch: ${GIT_BRANCH}<br>URL: ${GIT_URL}<br><hr><br>Build ID: ${BUILD_ID}<br>Build tag: ${BUILD_TAG}",
           "Charset": "UTF-8"
       }
   }
}
EOF
PWD=$(pwd)
/usr/bin/aws ses send-email \\
  --region eu-west-1 \\
  --from "Jenkins <no-reply@juspay.in>" \\
  --destination file://$PWD/ses_destination.json \\
  --message file://$PWD/ses_message.json'''
        sh "echo ${BUILD_URL}"
      }
    }
  }
}
