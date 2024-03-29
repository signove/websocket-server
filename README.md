# WebSocket Erlang Project #

Erlang Websocket project to enable real time communication.

### How it works? ###

* Introduction

* Realtime channel

* Configuration channel



### How do I get set up? ###

* Install NodeJS 10 
* Install npm
* Install Gulp `npm install -g gulp@3.9.0`
* Install Gulp dependencies `npm install`
* Install Erlang OTP_20.3.8.9 (http://www.erlang.org/download.html)
** I recommend you to use the Erlang Version Manager (https://github.com/robisonsantos/evm). Install these needed dependencies before install Erlang: `apt install libncurses5-dev autoconf gcc make`
** Optionally you can install the following packages for Erlang:
*** OpenSSL `apt install libssl-dev`
* Install Rebar 3 (http://www.rebar3.org). Use version 3.14.3 at minimum. (There is a copy of this rebar3 version at root project folder)
* Install Erlang/Rebar dependencies `gulp erlang:deps`
* Compile and execute the project `gulp run`
* See available tasks `gulp`

### Protocol communication ###

The protocol uses binary format to transfer data. It is as follows.

version:2 bytes|client_key: 20 bytes|payload: rest bytes

The methods (buffer = Utils.createMultiscreenMessage(receiver, payload)) and ({ version, sender, payload } = Utils.readMultiscreenMessage(buffer)) can be used to create a valid message.

### Deploy production version ###

To deploy a production version of the code in a server, follow the below steps:

* Execute the command `../rebar3 as prod tar` inside the `rebar` folder of the project source code
* Create a folder `multiscreen` folder in the `/opt/` folder of the computer to deploy
* Copy the generated .tar.gz file to the `/opt/multiscreen` folder and extract it
* Create a file `erlang-websocket-server.service` in the folder `/etc/systemd/system` folder with the following content:

```
[Unit]
Description=Erlang Websocket Server

[Service]
Type=simple
RemainAfterExit=yes
Environment=HOME=/opt/multiscreen
WorkingDirectory=/opt/multiscreen
ExecStart=/opt/multiscreen/bin/multiscreen_ws daemon
ExecStop=/opt/multiscreen/bin/multiscreen_ws stop

[Install]
WantedBy=multi-user.target
```

* Install and start the new created service with the following commands:

** `sudo systemctl start erlang-websocket-server.service`
** `sudo systemctl enable erlang-websocket-server.service`

### Deploy as Docker container ###

To deploy the code as a docker container, follow the below steps:

* Execute the command `docker build . -t websocket-server:latest`
* Then execute the command `docker run -d -p 8080:8080 websocket-server:latest`


### Usage ###

* A sample is available at http://<YOUR_IP_ADDRESS>:8080/multiscreen/sample.html).
