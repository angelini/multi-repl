from __future__ import unicode_literals

import sys
import msgpack


def is_expression(statement):
    try:
        compile(statement, '<string>', 'eval')
        return True
    except SyntaxError:
        return False


if __name__ == '__main__':
    unpacker = msgpack.Unpacker()
    packer = msgpack.Packer()

    while True:
        data = sys.stdin.read(1)
        unpacker.feed(data)

        for message in unpacker:
            result, error, etype = None, None, None
            statement = message['statement']

            try:
                if is_expression(statement):
                    result = eval(statement)
                else:
                    exec(statement)
            except Exception as e:
                etype = type(e).__name__
                error = e.message

            response = packer.pack({
                'result': result,
                'etype': etype,
                'error': error
            })
            sys.stdout.write(response)
            sys.stdout.flush()
