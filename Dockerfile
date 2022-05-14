FROM erlang:24.3.3
WORKDIR /app
COPY . ./

RUN apt-get update
RUN apt-get install curl libncurses5-dev autoconf gcc make libssl-dev -y
RUN curl -sL https://deb.nodesource.com/setup_16.x | bash
RUN apt-get install nodejs -y
RUN npm install -g grunt-cli
RUN npm install
RUN grunt erlang:deps
RUN grunt build
WORKDIR /app/rebar
CMD ["_build/default/rel/microservicehub/bin/microservicehub","foreground"]