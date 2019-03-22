"""
Decorate SCons Environment constructor so that MacPorts paths are always
included.

Source:
steelpangolin.wordpress.com/2014/02/06/modifying-scons-environment-variables-globally/
"""

from functools import wraps
import SCons.Environment

# MacPorts install directory
macports = '/opt/local'

flag_prepends = [
    ('CFLAGS',   '-I{}/include'.format(macports)),
    ('CXXFLAGS', '-I{}/include'.format(macports)),
    ('CPPFLAGS', '-I{}/include'.format(macports)),      # needed?
    ('LDFLAGS',  '-L{}/lib'.format(macports)),
    ('LINKFLAGS',  '-L{}/lib'.format(macports)),
]


def add_macports_vars(environment_init):
    @wraps(environment_init)
    def wrapper(self, *args, **kwargs):
        for var, flag in flag_prepends:
            flags_list = [flag]
            if var in kwargs:
                flags_list.append(kwargs[var])
            kwargs[var] = ' '.join(flags_list)

        environment_init(self, *args, **kwargs)

        self.PrependENVPath('PATH', '{}/bin'.format(macports))

    return wrapper


vars = SCons.Environment.Environment.__init__
SCons.Environment.Environment.__init__ = add_macports_vars(vars)
