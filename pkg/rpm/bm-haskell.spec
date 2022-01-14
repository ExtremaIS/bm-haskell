Name:          bm-haskell
Version:       {{VERSION}}
Release:       1%{?dist}
Summary:       Open bookmarks and queries from the command line
License:       MIT
URL:           https://github.com/ExtremaIS/bm-haskell
Source0:       bm-haskell-{{VERSION}}.tar.xz
BuildArch:     {{ARCH}}
BuildRequires: make
Requires:      glibc,gmp
#ExcludeArch:

%description
bm is a utility for opening bookmarks and queries from the command line.  It
allows you to quickly open bookmarks and perform search queries in your
browser using only your keyboard.

%global debug_package %{nil}

%prep
%setup -q

%build

%install
make install DESTDIR=%{buildroot} PREFIX=/usr

%check
make test

%files
%{_bindir}/bm
%{_mandir}/man1/bm.1.gz
%{_datadir}/doc/%{name}/

%changelog
* {{DATE}} {{RPMFULLNAME}} <{{RPMEMAIL}}> - {{VERSION}}-1
- Release {{VERSION}}
