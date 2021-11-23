FROM erlang:20.3.8
WORKDIR /app
COPY . ./

RUN apt-get update
RUN apt-get install curl libncurses5-dev autoconf gcc make libssl-dev
RUN curl -sL https://deb.nodesource.com/setup_4.x | bash
RUN apt-get install nodejs
RUN npm install -g gulp@3.9.0
RUN npm install
RUN gulp erlang:deps
RUN gulp build
WORKDIR /app/rebar
CMD ["_build/default/rel/multiscreen_ws/bin/multiscreen_ws","foreground"]