
## autoreconf -fiv
## yum install libtool yum install nasm


#rm -fr jpeg
#rm -fr jpeg-9c
#tar xzvf jpegsrc.v9c.tar.gz 
#cd jpeg-9c
#./configure --prefix=/raid5/tezancloud/IM/deps/eimp/deps/jpeg --enable-static
#make
#make install
#cd ..


rm -fr gd
rm -fr libjpeg-turbo-1.3.1
tar xzvf libjpeg-1.3.1.tar.gz
cd libjpeg-turbo-1.3.1
autoreconf -fiv
./configure --prefix=/raid5/tezancloud/IM/deps/eimp/deps/jpeg --enable-static
make
make install
cd ..

rm -fr libjpeg-turbo-1.3.1
 


rm -fr gd
rm -fr libgd-2.2.5
tar xzvf libgd-2.2.5.tar.gz 
cd libgd-2.2.5
./configure --with-jpeg=/raid5/tezancloud/IM/deps/eimp/deps/jpeg
make
make install
cd ..

rm -fr libgd-2.2.5
 