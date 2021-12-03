# Running a Docker image


## Running a pre-built image

To run a [Docker](https://www.docker.com) image of **STklos**, you can choose 
the version you want to use from
https://hub.docker.com/r/stklos/stklos. Version with the tag `latest` is built
from the Git repository and can be unstable. Versions with a numbered tag
correspond to the versions built from a stable release. Both images weight
approximately 15Mb.

**Example:**

```bash
$ docker pull stklos/stklos:1.70          # grab the 1.70 version of STklos
$ docker run -ti stklos/stklos:1.70       # and run it
...
stklos> (version)
"1.70"
stklos> (exit)
```

If you want to run a script file against such an image, you can bind the
directory containing your script to the `/home` directory (the current
directory of the docker image). For instance if you have a script called
`hello.stk` in your current directory, you can easily run it with a particular
version of STklos:

```bash
$ cat hello.stk
(display "Hello, world!\n")
$ docker run -v$(pwd):/home -ti stklos/stklos:1.40 \
        stklos -f hello.stk
Hello, world!
```
## Build your own Docker image

If you want to build your own Docker image, two docker files are provided in
the `./etc/Docker` directory of the source release. They contain the
instructions to build each image.

## The Scheme containers project

The [Scheme containers project](https://github.com/scheme-containers)
provides a large list of Docker images for various Scheme
implementations. The project offers nearly 50 implementations which are
all based on the same GNU/Linux distribution and an uniform interface. It is a
great way to test different implementations and compare them on the
same code. To run the **STklos** image built by this project:

```bash
$ docker pull schemers/stklos              # grab the latest version of STklos
$ docker run -ti schemers/stklos           # and run it
```

