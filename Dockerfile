FROM rocker/verse:3.5.0

RUN sudo apt-get update \
  && apt-get install -y clang \
  && apt-get install -y build-essential \
  && apt-get install -y python3-pip python3-dev \
  && cd /usr/local/bin \
  && ln -s /usr/bin/python3 python \
  && pip3 install --upgrade pip \
  && apt-get install -y jags
  
RUN install2.r --error --deps=TRUE \
	rjags

RUN r -e 'devtools::install_github("tereom/quickcountmx", ref="election_day")'

CMD ["python", "/home/rstudio/quickcountmx/scripts/monitor.py", "/home/rstudio/quickcountmx/work_dir/datos", "/home/rstudio/quickcount/work_dir/salidas",  "5"]