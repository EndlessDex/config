#! /bin/bash
if ping -q -c 1 -W 1 8.8.8.8 > /dev/null; then
    wget -q https://source.unsplash.com/random/1920x1080 -O ~/backPic1.jpg;
    wget -q https://source.unsplash.com/random/1680x1050 -O ~/backPic2.jpg;
fi