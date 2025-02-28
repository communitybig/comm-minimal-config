# Maintainer: Tales A. Mendonça <talesam@gmail.com>

pkgname=comm-minimal-config
pkgdesc="Minimal configuration package"
depends=('bash' 'filesystem' 'kbd')
#makedepends=('')
#optdepends=('')
conflicts=('big-skel')
#provides=('')
#replaces=('')
pkgver=$(date +%Y%m%d)
pkgrel=$(date +%H%M)
arch=('any')
license=('MIT')
url="https://github.com/communitybig/${pkgname}"
source=("git+${url}.git")
md5sums=('SKIP')
install="${pkgname}.install"

prepare() {
    cd "${srcdir}/${pkgname}"
    
    # Create an alternative structure for the files
    mkdir -p "${srcdir}/${pkgname}/custom-configs"
    
    # Move conflicting files to the custom-configs folder
    if [ -d "${srcdir}/${pkgname}/etc" ]; then
        mv "${srcdir}/${pkgname}/etc" "${srcdir}/${pkgname}/custom-configs/"
    fi
    
    if [ -d "${srcdir}/${pkgname}/usr" ]; then
        mv "${srcdir}/${pkgname}/usr" "${srcdir}/${pkgname}/custom-configs/"
    fi
}

build() {
    cd "${srcdir}/${pkgname}"
    # No compilation required
}

check() {
    cd "${srcdir}/${pkgname}"
    # No tests need to be run
}

package() {
    cd "${srcdir}/${pkgname}"

    # Determine the correct source directory
    if [ -d "${pkgname}" ]; then
        srcdir="${srcdir}/${pkgname}/${pkgname}"
    else
        srcdir="${srcdir}/${pkgname}"
    fi

    # Install the custom configuration files into the custom-configs folder
    if [ -d "${srcdir}/custom-configs" ]; then
        install -dm755 "${pkgdir}/usr/share/${pkgname}"
        cp -a "${srcdir}/custom-configs" "${pkgdir}/usr/share/${pkgname}/"
    fi

    # Install the license file if present
    if [ -f "LICENSE" ]; then
        install -Dm644 LICENSE "${pkgdir}/usr/share/licenses/${pkgname}/LICENSE"
    fi

    # Install the documentation if present
    if [ -f "README.md" ]; then
        install -Dm644 README.md "${pkgdir}/usr/share/doc/${pkgname}/README.md"
    fi
}