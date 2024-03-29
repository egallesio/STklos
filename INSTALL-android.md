# Installing STklos on Android

## CAVEAT: thread-kill! is limited on Android

This is because `pthread_cancel` is not implemented on Android.
`thread-kill!` is restricted so it can only kill its own thread
(that is, a thread can kill itself), and threads that are not in
running state.

## Overview

Instead of cross-compiling, compile STklos in the Android device itself using
the Termux package, which has all build tools available. This means the following
is needed:

- You must have developer access and enable ADB debugging on the phone
- The Termux package must be installed on the device

It is possible to do everything in the android device, typing all commands on
Termux, but we also assume you will want to access the device via ssh, which
is more comfortable.


## Install adb

We assume you are connecting to the Android device from some machine (a PC or notebook),
which we will call the "host"

On the host, install the Android Debug Bridge (abd), with

```
sudo apt install adb
```

```
yum install android-tools
```

```
sudo pacman -S android-tools
```

or the equivalent for your system.

You will need to enable debugging in the Android device.

Test with a USB cable

* `adb devices` should list your Android device (you will need to authorize the connection on the device screen)
* `adb shell` gives you a shell on the device.

Or, if you enabled ADB debugging via Wifi,

```
adb tcpip 5555
adb connect <PHONE-IP>:5555
```

Then do `adb devices` and `adb shell` to test the connection.

You do NOT need the adb shell -- we will be using the Termux shell. Adb will be used solely
to forward TCP connections so we can ssh into the device.

## Set up sshd in Termux

Open Termux in the Android device and update its packages.

```
pkg update
pkg upgrade
```

Now prepare it to accept ssh connections:

```
pkg install openssh
mkdir .ssh
cat > .ssh/authorized_keys
```

Paste your public ssh key there.

Now fix the permissions on `.ssh` and `.ssh/authorized_keys`:

```
chmod 600 .ssh/authorized_keys
chmod 700 .ssh
```

Termux does not start anything automatically, so every time we connect,
we need to call `sshd`:

```
sshd
```

## From the host:

ADB can forward the TCP port 8022 to the devices' 8022 port (which is where Termux's sshd listens by default).

```
adb forward tcp:8022 tcp:8022
ssh localhost -p 8022
```

You are now remotely connected to the Termux shell.

## Prepare to use the external SD card (optional)

If you don't want compilation to use the devices' internal flash memory, you may
use your external SD card for that. Run

```
termux-setup-storage
```

and you will notice that a `storage` folder will be created on your homedir in Termux:

```
ls -l storage/

lrwxrwxrwx 1 u0_a196 u0_a196 24 Jun 28 07:33 dcim -> /storage/emulated/0/DCIM
lrwxrwxrwx 1 u0_a196 u0_a196 28 Jun 28 07:33 downloads -> /storage/emulated/0/Download
lrwxrwxrwx 1 u0_a196 u0_a196 75 Jun 28 07:33 external-1 -> /storage/4c14af23-3e4f-4511-acfd6-c1af4dc792e0/Android/data/com.termux/files
lrwxrwxrwx 1 u0_a196 u0_a196 26 Jun 28 07:33 movies -> /storage/emulated/0/Movies
lrwxrwxrwx 1 u0_a196 u0_a196 25 Jun 28 07:33 music -> /storage/emulated/0/Music
lrwxrwxrwx 1 u0_a196 u0_a196 28 Jun 28 07:33 pictures -> /storage/emulated/0/Pictures
lrwxrwxrwx 1 u0_a196 u0_a196 19 Jun 28 07:33 shared -> /storage/emulated/0
```

If you get a "permission denied" error, or if the `storage/` directory was not
created, please try revoking and granting again permissions to the Termux app
in your Android configuration (see https://wiki.termux.com/wiki/Termux-setup-storage).

Now `cd storage/external-1` and you will be in the SD card directory (you won't
see the contents of the SD card there, Android doesn't let you do that -- but
you are in a directory specifically created for Termux in your extenal SD card).

## Compile STklos

After ssh-ing into the device, install the build tools. Termux does not have gcc, only clang.

```
apt install libgc libpcreposix make clang git autoconf automake
```

Of course, clone the repository (or you can download a release tarball and extract it):

```
git clone https://github.com/egallesio/STklos
```

Configure: in order to install STklos for usage within Termux, we use the prefix as below.

```
./configure --prefix=/data/data/com.termux/files/usr/
make
make test
```

All tests should pass. Now install STklos (without sudo -- the prefix chosen above
is in your Termux home):

```
make install
```

Now run STklos!


```
stklos -i
  \    STklos version 1.40  (Id: 58dfbe0)
   \   Copyright (C) 1999-2020 Erick Gallesio <eg@unice.fr>
  / \  Université Côte d'Azur
 /   \ [Linux-3.18.140-perf-g1b0d9102ccae-aarch64/none/readline/utf8]
stklos>
```

`stklos-pkg` and `stklos-compile` should also work.


-------------------------------------------------

## Installing STklos on an virtual Android device

Installing STklos on an *emulated* Android device can also be useful for testing/developing purpose. You can use

- an emulator such as anbox (Android in a box)
- a VM running an X86 Android (such as Bliss-OS which is based on Android 10 version)

### Running on anbox

#### Download and install `anbox`

In Debian, you may just `sudo apt install anbox`.

Get an Android image for your architecture (for example amd64) and place it in
`/var/lib/anbox`.

```
wget https://build.anbox.io/android-images/2018/07/19/android_amd64.img
cp android_amd64.img /var/lib/anbox/android.img
```

Start the anbox service:

```
sudo systemctl start anbox-container-manager.service
systemctl --user start anbox-session-manager.service
```

Use anbox to launch the app manager, where you will be able to see a screen with
all pre-installed apps:

```
anbox launch --package=org.anbox.appmgr --component=org.anbox.appmgr.AppViewActivity
```
#### Install FDroid and Termux

Install F-droid using adb (you can install any `.apk` file via adb, actually):

```
wget https://f-droid.org/FDroid.apk
adb install FDroid.apk
```

```
anbox launch --package=org.anbox.appmgr --component=org.anbox.appmgr.AppViewActivity
```

Open FDroid, update its package list, then install Termux (and any other apps you'd like!)

Open Termux from the emulator and do the following:

```
pkg update
pkg upgrade
```

#### Compile and install STklos

Now, either follow the instructions given above for compiling STklos (be aware that Termux has no access to your desktop's clipboard, so you cannot copy and paste between them).

Install the dependencies:

```
apt install libgc libpcreposix make clang git autoconf automake
```

Clone STklos repository:

```
git clone https://github.com/egallesio/STklos
```

Configure, `make`, `make test` and `make install`:

```
./configure --prefix=/data/data/com.termux/files/usr/
make
make test
make install
```

(Do NOT use `sudo` to install; it is not necessary in this Termux setup)



### Running an x86 Android OS on a VM

Download an ISO file of an x86 Android OS and install it on your preferred VM system.
Once you have it running, you should have access to the Google Play Store. Then, you can install a version of `termux` on your running OS. Compiling and running STklos, can be done as explained before with the anbox tool or a native  installation. 

Have fun
