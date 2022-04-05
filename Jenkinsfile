import groovy.transform.Field

@Field
def abiList = ['armeabi-v7a', 'arm64-v8a', 'x86', 'x86_64']

@Field
def commitSha = ""

@Field
def artifactList = ['spell-dict', 'chinese-addons-data', 'libime', 'fmt', 'libevent', 'libintl-lite', 'lua', 'boost']

def setBuildStatus(String message, String state, String ctx, String commitSha) {
    withCredentials([string(credentialsId: 'github-commit-status-token', variable: 'token')]) {
        def body = """{
             "state": "$state",
             "description": "$message",
             "context": "$ctx",
             "target_url": "$BUILD_URL"
          }
        """.toString()
        httpRequest consoleLogResponseBody: true,
                contentType: 'APPLICATION_JSON',
                httpMode: 'POST',
                requestBody: body,
                url: "https://api.github.com/repos/fcitx5-android/prebuilder/statuses/$commitSha".toString(),
                validResponseCodes: '201',
                customHeaders: [[name: 'Authorization', value: "token " + token]]
    }
}

def sendMessageToTelegramGroup(String message) {
    withCredentials([string(credentialsId: 'fcitx5-android-telegram-group', variable: 'chatId'),
                     string(credentialsId: 'fcitx5-android-telegram-bot', variable: 'token')]) {
        def body = """{
             "chat_id": $chatId,
             "text": "${message.replace("-", "\\\\-")}",
             "parse_mode": "MarkdownV2",
             "disable_web_page_preview": true
          }
        """.toString()
        httpRequest consoleLogResponseBody: true,
                contentType: 'APPLICATION_JSON',
                httpMode: 'POST',
                requestBody: body,
                url: "https://api.telegram.org/bot$token/sendMessage".toString(),
                validResponseCodes: '200'
    }
}

def withBuildStatus(String name, Closure closure) {
    def ctx = "Jenkins Build / $name"
    stage(name) {
        try {
            setBuildStatus("...", "pending", ctx, commitSha)
            def start = System.currentTimeMillis()
            closure()
            def end = System.currentTimeMillis()
            setBuildStatus("Successful in ${(end - start) / 1000} seconds", "success", ctx, commitSha)
        } catch (Exception e) {
            setBuildStatus("Failed", "failure", ctx, commitSha)
            throw e
        }
    }
}


node("android") {
    catchError {
        timestamps {
            try {
                stage("Fetching sources") {
                    checkout([$class                           : 'GitSCM',
                              branches                         : scm.branches,
                              doGenerateSubmoduleConfigurations: false,
                              extensions                       : [[$class             : 'SubmoduleOption',
                                                                   disableSubmodules  : false,
                                                                   parentCredentials  : true,
                                                                   recursiveSubmodules: true,
                                                                   reference          : '',
                                                                   trackingSubmodules : false]],
                              submoduleCfg                     : [],
                              userRemoteConfigs                : scm.userRemoteConfigs])
                    sh "git config --get remote.origin.url > .git/remote-url"
                    repoUrl = readFile(".git/remote-url").trim()
                    sh "git rev-parse HEAD > .git/current-commit"
                    commitSha = readFile(".git/current-commit").trim()
                    setBuildStatus("...", "pending", "Jenkins Build", commitSha)
                    sh 'rm -rf build prebuilt'
                    sh 'mkdir build prebuilt'
                }

                withBuildStatus("Build everything") {
                    dir('build') {
                        withEnv(["ABI=${abiList.join(',')}", "ANDROID_PLATFORM=$platform"]) {
                            sh 'runghc ../Main.hs -j everything'
                        }
                    }
                }

                withBuildStatus("Push to fcitx5-android-prebuilt-libs") {
                    dir('prebuilt') {
                        withCredentials([string(credentialsId: 'github-commit-status-token', variable: 'token')]) {
                            sh 'git init'
                            sh 'git config user.name Jenkins'
                            sh 'git config user.email 102923727+android-fcitx5@users.noreply.github.com'
                            sh 'git remote add origin https://$token@github.com/fcitx5-android/fcitx5-android-prebuilt-libs.git'
                            sh 'git fetch origin'
                            sh 'git checkout master'
                            sh "rm -rf ${artifactList.join(' ')}"
                            sh "cp -a ../build/{${artifactList.join(',')}} ./"
                            sh 'git add .'
                            sh 'git commit -m "Auto update"'
                            sh 'git push --set-upstream origin "HEAD:master" --follow-tags --atomic'
                        }
                    }
                }

                stage("Post build (success)") {
                    setBuildStatus("Successful", "success", "Jenkins Build", commitSha)
                    sendMessageToTelegramGroup("[${JOB_NAME}-${BUILD_NUMBER}](${BUILD_URL}) succeeded")
                }

            } catch (Exception e) {
                stage("Post build (failure)") {
                    setBuildStatus("Failed", "failure", "Jenkins Build", commitSha)
                    sendMessageToTelegramGroup("[${JOB_NAME}-${BUILD_NUMBER}](${BUILD_URL}) failed")
                    throw e
                }
            }

        }
    }
}
