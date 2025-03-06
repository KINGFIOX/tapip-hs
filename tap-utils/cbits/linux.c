#include <fcntl.h>
#include <string.h>
#include <sys/ioctl.h>
#include <unistd.h>

#include <linux/if_tun.h>
#include <linux/in.h>
#include <linux/socket.h>
#include <net/if.h>

#define ETH_ALEN 6

int setnetmask_tap(int skfd, const unsigned char *name, unsigned int netmask) {
  int ret;
  struct ifreq ifr = {};
  struct sockaddr_in *saddr;

  strcpy(ifr.ifr_name, (char *)name);
  saddr = (struct sockaddr_in *)&ifr.ifr_netmask;
  saddr->sin_family = AF_INET;
  saddr->sin_addr.s_addr = netmask;
  if ((ret = ioctl(skfd, SIOCSIFNETMASK, (void *)&ifr) < 0)) {
    return ret;
  }
  return 0;
}

static int setflags_tap(int skfd, const unsigned char *name,
                        unsigned short flags, int set) {
  int ret;
  struct ifreq ifr = {};

  strcpy(ifr.ifr_name, (char *)name);
  /* get original flags */
  if ((ret = ioctl(skfd, SIOCGIFFLAGS, (void *)&ifr)) < 0) {
    return ret;
  }
  /* set new flags */
  if (set) {
    ifr.ifr_flags |= flags;
  } else {
    ifr.ifr_flags &= ~flags & 0xffff;
  }
  if ((ret = ioctl(skfd, SIOCSIFFLAGS, (void *)&ifr)) < 0) {
    return ret;
  }
  return 0;
}

int setup_tap(int skfd, const unsigned char *name) {
  return setflags_tap(skfd, name, IFF_UP | IFF_RUNNING, 1);
}

int getmtu_tap(int skfd, const unsigned char *name, int *mtu) {
  int ret;
  struct ifreq ifr = {};

  strcpy(ifr.ifr_name, (char *)name);
  /* get net order hardware address */
  if ((ret = ioctl(skfd, SIOCGIFMTU, (void *)&ifr)) < 0) {
    return ret;
  }
  *mtu = ifr.ifr_mtu;
  return 0;
}

int setipaddr_tap(int skfd, const unsigned char *name, unsigned int ipaddr) {
  int ret;
  struct ifreq ifr = {};
  struct sockaddr_in *saddr;

  strcpy(ifr.ifr_name, (char *)name);
  saddr = (struct sockaddr_in *)&ifr.ifr_addr;
  saddr->sin_family = AF_INET;
  saddr->sin_addr.s_addr = ipaddr;
  if ((ret = ioctl(skfd, SIOCSIFADDR, (void *)&ifr) < 0)) {
    return ret;
  }
  return 0;
}

int getipaddr_tap(int skfd, const unsigned char *name, unsigned int *ipaddr) {
  int ret;
  struct ifreq ifr = {};
  struct sockaddr_in *saddr;

  strcpy(ifr.ifr_name, (char *)name);
  if ((ret = ioctl(skfd, SIOCGIFADDR, (void *)&ifr)) < 0) {
    return ret;
  }
  saddr = (struct sockaddr_in *)&ifr.ifr_addr;
  *ipaddr = saddr->sin_addr.s_addr;
  return 0;
}

int getname_tap(int tapfd, unsigned char *name) {
  int ret;
  struct ifreq ifr = {};
  if ((ret = ioctl(tapfd, TUNGETIFF, (void *)&ifr) < 0)) {
    return ret;
  }
  strcpy((char *)name, ifr.ifr_name);
  return 0;
}

int gethwaddr_tap(int tapfd, unsigned char *ha) {
  int ret;
  struct ifreq ifr = {};
  /* get net order hardware address */
  if ((ret = ioctl(tapfd, SIOCGIFHWADDR, (void *)&ifr) < 0)) {
    return ret;
  }
  memcpy(ha, ifr.ifr_hwaddr.sa_data, ETH_ALEN);
  return 0;
}

int alloc_tap(int tapfd, const char *dev) {
  int ret;
  struct ifreq ifr = {};

  ifr.ifr_flags = IFF_TAP | IFF_NO_PI;
  if (*dev) {
    strncpy(ifr.ifr_name, dev, IFNAMSIZ);
  }
  /*
   * create a new tap device
   * if created already, just bind tun with file
   */
  if ((ret = ioctl(tapfd, TUNSETIFF, (void *)&ifr) < 0)) {
    return ret;
  }

  return 0;
}

int set_tap(void) { return socket(PF_INET, SOCK_DGRAM, IPPROTO_IP); }

int setpersist_tap(int fd) { return ioctl(fd, TUNSETPERSIST, 1); }
