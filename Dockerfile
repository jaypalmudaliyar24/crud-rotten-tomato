FROM haskell:latest
RUN apt update
RUN apt install -y postgresql libpq-dev
VOLUME /root/.stack
VOLUME /usr/src/app/src
# Set the working directory in the container
WORKDIR /usr/src/app
# Copy the local project files to the container
COPY . .
# RUN stack setup
# RUN stack build
CMD ["stack", "run"]
# CMD ["bash"]
EXPOSE 8081