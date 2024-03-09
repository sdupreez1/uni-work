import matplotlib.pyplot as plt
from matplotlib.widgets import Slider
import numpy as np


def f_r(r, x):      # logistic map
    return r*x*(1-x)


def dfdx(f, x): # find derivative of f
    if type(x) == 'numpy.complex128':
        h = complex(0.001, 0.001)
        return (f(x + h) - f(x))/h
    else:
        h = 0.001
        return (f(x + h) - f(x)) / h


def fn(f, n, x):        # iterate f on itself n times
    if n == 0:
        return x
    else:
        return fn(f, n-1, f(x))

def quadplot():
    r_init = 4
    x = np.linspace(0, 1, 1000)

    fig, ax = plt.subplots()    # what do the commas do???????????
    parabola, = plt.plot(x, f_r(r_init, x))
    plt.plot(x, x, color='red')

    fig.subplots_adjust(bottom=0.25)

    axfreq = fig.add_axes([0.25, 0.1, 0.65, 0.03])
    r_slider = Slider(
        ax=axfreq,
        label='r value',
        valmin=0,
        valmax=4,
        valinit=r_init,
    )

    def update(val):
        parabola.set_ydata(f_r(r_slider.val, x))
        fig.canvas.draw_idle()

    r_slider.on_changed(update)

    plt.show()

def f_c(z, c):
    return z**2 + c


def julia(f,    # f = f(z, c), c = parameter
          c,    # parameter value
          n,    # n = number of iterations to check if f^i is bounded for i < n
          re_lims,  # real axis bounds
          im_lims,  # imaginary axis bounds
          res   # resolution
          ):     # code taken from https://www.cantorsparadise.com/the-julia-set-e03c29bed3d0

    x, y = np.meshgrid(np.linspace(re_lims[0], re_lims[1], res),
                       np.linspace(im_lims[0], im_lims[1], res))

    K = np.zeros([res,res])     # K will become filled in Julia set

    z = x + y * 1j

    for i in range(n):       # checks if i'th iterate of f(z) is less than np.inf and colours the pixel corresponding to z if it is
        z = f(z, c)          # i.e if f^i(z) is bounded for all i < n then we include it in the filled in julia set K(f)
        k = np.abs(z) < np.inf
        K[k] += 1

    plt.figure(figsize=(7, 7))
    plt.pcolormesh(x, y, K, cmap='RdGy', shading='auto')
    plt.axis('equal')
    plt.axis('off')
    plt.show()


def mandelbrot(n,   # number of iterations to check if f^i is bounded for i < n
               re_lims, # real axis bounds
               im_lims, # imaginary axis bounds
               res  # resolution
               ):

    cvals_re, cvals_im = np.meshgrid(np.linspace(re_lims[0], re_lims[1], res),
                                     np.linspace(im_lims[0], im_lims[1], res))

    cvals = cvals_re + cvals_im * 1j

    M = np.zeros([res, res])

    z = 0
    for i in range(n):  # Julia set algorithm but over cvals rather than z
        z = f_c(z, cvals)
        k = np.abs(z) < np.inf
        M[k] += 1

    plt.figure(figsize=(6, 6))
    plt.pcolormesh(cvals_re, cvals_im, M, cmap='RdGy', shading='auto')
    plt.axis('equal')
    plt.axis('off')
    plt.show()


julia(f_c, -1.3, 50, [-2, 2], [-2, 2], 1000)


mandelbrot(50, [-2.5, 1], [-2, 2], 1000)

