# python3
sudo port -N install python36
sudo port select --set python python36
sudo port select --set python3 python36

# pip3
sudo port -N install py36-pip
sudo port select --set pip pip36
sudo port select --set pip3 pip36


# python2
sudo port -N install python27
sudo port select --set python2 python27

# pip2
sudo port -N install py27-pip
sudo port select --set pip2 pip27
