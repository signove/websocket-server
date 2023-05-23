# WebSocket Erlang Project #

Erlang Websocket project to enable real time communication.

### How it works? ###

* Introduction

* Realtime channel

* Configuration channel



### How do I get set up? ###

* Install NodeJS 10 
* Install npm
* Install Grunt `npm install -g grunt-cli@1.4.3`
* Install Grunt dependencies `npm install`
* Install Erlang OTP_24.3.3.3 (http://www.erlang.org/download.html)
** I recommend you to use the Erlang Version Manager (https://github.com/robisonsantos/evm). Install these needed dependencies before install Erlang: `apt install libncurses5-dev autoconf gcc make`
** Optionally you can install the following packages for Erlang:
*** OpenSSL `apt install libssl-dev`
* Install Rebar 3 (http://www.rebar3.org). Use version 3.14.3 at minimum. (There is a copy of this rebar3 version at root project folder)
** You can copy the given `rebar3` file to bin folder `sudo cp rebar3 /usr/local/bin`
* Install Erlang/Rebar dependencies `grunt erlang:deps`
* Compile and execute the project `grunt build`
* See available tasks `grunt`

### Protocol communication ###

The protocol uses binary format to transfer data. It is as follows.

version:2 bytes|client_key: 20 bytes|payload: rest bytes

The methods (buffer = Utils.createMicroServiceHUBMessage(receiver, payload)) and ({ sender, payload } = Utils.readMicroServiceHUBMessage(buffer)) can be used to create a valid message.

### Deploy production version ###

To deploy a production version of the code in a server, follow the below steps:

* Execute the command `../rebar3 as prod tar` inside the `rebar` folder of the project source code
* Create a folder `microservicehub` folder in the `/opt/` folder of the computer to deploy
* Copy the generated .tar.gz file to the `/opt/microservicehub` folder and extract it
* Create a file `erlang-websocket-server.service` in the folder `/etc/systemd/system` folder with the following content:

```
[Unit]
Description=Erlang Websocket Server

[Service]
Type=simple
RemainAfterExit=yes
Environment=HOME=/opt/microservicehub
WorkingDirectory=/opt/microservicehub
ExecStart=/opt/microservicehub/bin/microservicehub daemon
ExecStop=/opt/microservicehub/bin/microservicehub stop

[Install]
WantedBy=multi-user.target
```

* Install and start the new created service with the following commands:

** `sudo systemctl start erlang-websocket-server.service`
** `sudo systemctl enable erlang-websocket-server.service`

### Deploy as Docker container ###

To deploy the code as a docker container, follow the below steps:

* Execute the command `docker build . -t microservice-hub:latest`
* Then execute the comma1nd `docker run -d -p 8080:8080 microservice-hub:latest`


### Usage ###

* A sample is available at http://<YOUR_IP_ADDRESS>:8080/microservicehub/sample.html).
