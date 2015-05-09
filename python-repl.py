from __future__ import unicode_literals

import sys
import msgpack


def run_message(message):
    try:
        return {'result': eval(message['statement']),
                'error': None}
    except Exception as e:
        return {'result': None,
                'error': e.message}


if __name__ == '__main__':
    unpacker = msgpack.Unpacker()
    packer = msgpack.Packer()

    while True:
        data = sys.stdin.read(1)
        unpacker.feed(data)

        for message in unpacker:
            result = packer.pack(run_message(message))
            sys.stdout.write(result)
            sys.stdout.flush()
