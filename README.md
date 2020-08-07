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
* Install Erlang OTP_20.0 (http://www.erlang.org/download.html)
** I recommend you to use the Erlang Version Manager (https://github.com/robisonsantos/evm). Install these needed dependencies before install Erlang: `apt install libncurses5-dev autoconf`
** Optionally you can install the following packages for Erlang:
*** OpenSSL `apt install libssl-dev`
* Install Rebar 3 (http://www.rebar3.org)
* Install Erlang/Rebar dependencies `gulp erlang:deps`
* Compile and execute the project `gulp run`
* See available tasks `gulp`

### Usage ###

* A sample is available at http://<YOUR_IP_ADDRESS>:8080/multiscreen/sample.html).
