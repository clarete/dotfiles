import io
import os.path
import sys
import gnupg


def parse_kv(line):
    try:
        _, key, _, _, _, user, _, password = line.split()
        return (key, password[1:-1])
    except ValueError:
        return (None, None)

def parse_authinfo(data):
    return dict(parse_kv(line) for line in data.splitlines())

def get_sec(key):
    gpg = gnupg.GPG(gnupghome=os.path.expanduser('~/.gnupg'))
    with io.open(os.path.expanduser('~/.authinfo.gpg'), 'rb') as fd:
        return parse_authinfo(gpg.decrypt_file(fd).data)[key]

if __name__ == '__main__' and len(sys.argv) == 2:
    print(get_sec(sys.argv[1]))
