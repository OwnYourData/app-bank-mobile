FROM oydeu/oyd-shiny_armhf:latest
MAINTAINER "Christoph Fabianek" christoph@ownyourdata.eu

RUN cd /srv/shiny-server \
        && rm -rf * \
        && git clone https://github.com/OwnYourData/app-template-mobile.git \
        && mv app-template-mobile/* . \
        && rm -r app-template-mobile \
        && R -e "source('init.r')"

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]