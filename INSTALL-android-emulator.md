# Installing STklos on an emulated Android device

It is possible to install STklos on an Android device (see `INSTALL-android.md`
for instructions), or on an *emulated* Android device -- which is what this
file explains.



## Download and install `anbox`

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

## Install FDroid and Termux

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

## Compile and install STklos

Now, either follow the instructions in the file `INSTALL-android.md` (starting from
the section ) in order to access the emulated system via ssh, or keep reading this
document to work directly on Termux -- but be aware that Termux has no access to
your desktop's clipboard, so you cannot copy and paste between them.

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

