pipeline {
  agent {
    label 'nix'
  }

  stages {
    stage('Build and Test') {
      steps {
        sh 'nix-build -A euler-events-hs --option sandbox false --arg remoteDeps true'
      }
    }
    stage('Build docker') {
      steps {
        sh 'make build -e VERSION=$(git rev-parse --short HEAD)'
      }
    }
    /* stage('Deploy euler-api-order') {
      environment {
        BUILD_VERSION="""${sh(
            returnStdout: true,
            script: 'git rev-parse --short HEAD'
        )}"""
      }
      stages {
        stage('Docker Load') {
          steps {
            sh 'make load -e VERSION=$(git rev-parse --short HEAD)'
          }
        }
        stage('Docker Push') {
          steps {
            sh 'make push -e VERSION=$(git rev-parse --short HEAD)'
          }
        }
      }
    } */
    stage('Summary') {
      steps {
        sh '''cat << EOF > ses_destination.json
{
  "ToAddresses":  ["sre-team@juspay.in"],
  "CcAddresses":  ["infra@juspay.in"],
  "BccAddresses": []
}
EOF
GIT_REV=$(git rev-parse --short HEAD)
cat << EOF > ses_message.json
{
   "Subject": {
       "Data": "Jenkins: ${JOB_NAME} / Build: (${BUILD_NUMBER}) / Completed",
       "Charset": "UTF-8"
   },
   "Body": {
       "Html": {
           "Data": "Build - <a href=\\"${BUILD_URL}\\" target=\\"_blank\\">${JOB_NAME}/#${BUILD_NUMBER}</a> is completed successfully.<br><hr><br>Docker Image: asia.gcr.io/jp-k8s-internal/euler-events-hs:${GIT_REV}<br><br>Git Commit: ${GIT_COMMIT}<br>Git branch: ${GIT_BRANCH}<br>URL: ${GIT_URL}<br><hr><br>Build ID: ${BUILD_ID}<br>Build tag: ${BUILD_TAG}",
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
