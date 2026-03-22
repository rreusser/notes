import sys

def fix_fortran_formatting(input_stream):
    for line in input_stream:
        if line.strip().startswith('*'):
            print(line.strip())
        else:
            print('      ' + line, end='')

if __name__ == "__main__":
    if len(sys.argv) > 1:
        with open(sys.argv[1], 'r') as file:
            fix_fortran_formatting(file)
    else:
        fix_fortran_formatting(sys.stdin)