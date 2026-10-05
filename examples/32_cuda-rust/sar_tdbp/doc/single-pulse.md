explain what the decoded picture depicts with only 1/1024 pulses. i see a circle (or ellipse) with sidebands

What you are seeing is an **isorange circle (range arc)** modulated by the **sinc sidelobes** of your radar pulse. 

Here is the exact physical and mathematical explanation of why it appears this way with only 1 pulse:

---

### 1. Why a circle or arc? (Zero Azimuth Resolution)
* **Single antenna position:** With only 1 pulse ($p = 0$), the radar antenna is located at a single fixed coordinate in 3D space: 
  $$\mathbf{P}_0 = (x_0, y_0, z_0) = (-20.0\,\text{m},\, 0.0\,\text{m},\, 100.0\,\text{m})$$
* **Ambiguity:** A single radar pulse measures only the **round-trip travel time** $\tau = \frac{2R}{c}$ (i.e. distance $R$). It has no antenna array or synthetic baseline to distinguish *which direction* on the ground the echo returned from.
* **The Ground Intersection:** The locus of all ground points $(x, y, z=0)$ at distance $R$ from the antenna satisfies:
  $$(x - x_0)^2 + (y - 0)^2 + (0 - h)^2 = R^2 \implies (x - x_0)^2 + y^2 = R^2 - h^2$$
  Since $R^2 - h^2 = r_{\text{ground}}^2$ is constant, this is geometrically a **circle on the ground plane**, centered at $(x_0, 0)$. When mapped to your rectangular scene window, it cuts through as a curved circular/elliptical arc.

In short: **1 pulse has range resolution, but zero azimuth (cross-range) resolution**, so it smears the energy of each scatterer along an entire circle of constant distance.

---

### 2. What are the "sidebands"? (Sinc Sidelobes)
In `03_simulator.rs`, the range-compressed echo is modeled with normalized sinc interpolation:
$$s(d) \propto \operatorname{sinc}\left(\frac{2B}{c}(d - R)\right)$$

* A sinc function does not drop instantly to zero; it has periodic decaying **sidelobes** (the first sidelobe is only $\approx -13.3\text{ dB}$ below the peak).
* Because the visualization (`image_to_rgb`) applies a **$40\text{ dB}$ dynamic range** colormap, these sidelobes are clearly visible as **parallel concentric bands** on either side of the main circular crest.
* *(Note: If you are running the default `grid` phantom with 25 points or `rust` with 58 points, each scatterer also creates its own concentric circular ring corresponding to its respective distance from the radar).*

---

### 3. How SAR focuses this into points
As you step up the aperture toward 1024 pulses:
1. The platform moves along the $x$-axis from $-20\,\text{m}$ to $+20\,\text{m}$.
2. Every subsequent pulse projects another circular arc, but **centered at its new antenna position**.
3. The matched filter term ($e^{+j \frac{4\pi d}{\lambda}}$) in the backprojection kernel ensures that all these circles **add constructively in-phase** at the true target position $(x_t, y_t)$, while **interfering destructively** everywhere else along the arcs.

The circular bands gradually cancel out, collapsing into sharp, focused point scatterers.



why is it a sinc? is it because the chirp has a finite length?

**Yes, but with an important distinction:** it is a $\text{sinc}$ specifically because the finite length of the chirp limits it to a **finite, rectangular band of frequencies**. 

The fundamental reason is Fourier duality: **a rectangle in the frequency domain transforms into a $\text{sinc}$ in the time domain.**

Here is how that happens step-by-step:

---

### 1. The Frequency-Domain View (The Core Reason)

A linear frequency modulated (LFM) chirp sweeps its frequency at a constant rate $K$ over a finite duration $T$:
$$f(t) = f_0 + K t \quad \text{for } -\frac{T}{2} \le t \le \frac{T}{2}$$

Because the frequency changes linearly and the amplitude is constant:
1. Every frequency in the bandwidth $B = |K|T$ is visited for the **exact same amount of time**.
2. This means the power spectrum of the pulse is essentially a flat, sharp "boxcar" or **rectangular window** of width $B$:
   $$|S(f)|^2 \approx \text{rect}\left(\frac{f}{B}\right)$$

When the radar performs **pulse compression** (matched filtering), the output is the autocorrelation of the chirp. By the Wiener–Khinchin theorem, the autocorrelation is the **inverse Fourier transform of the power spectrum**:

$$\mathcal{F}^{-1}\left\{\text{rect}\left(\frac{f}{B}\right)\right\} = B \cdot \operatorname{sinc}(B t)$$

---

### 2. The Time-Domain View (Where Finite Length Enters)

If you calculate the autocorrelation directly in the time domain by sliding the finite-length chirp over its conjugate:

$$R(\tau) = \int s(t) s^*(t - \tau) \, dt$$

Because the pulse is truncated to length $T$, the overlapping region between the two signals shrinks linearly as $|\tau|$ increases. The exact mathematical autocorrelation is:

$$R(\tau) = \left(1 - \frac{|\tau|}{T}\right) \operatorname{sinc}\left(B \tau \left(1 - \frac{|\tau|}{T}\right)\right) \quad \text{for } |\tau| \le T$$

* Radar chirps have a huge **time-bandwidth product** (typically $T \cdot B \gg 100$).
* This means the compressed pulse width ($\sim 1/B$) is tiny compared to the pulse duration $T$.
* Over the main peak and the first dozens of sidelobes, $\frac{|\tau|}{T} \approx 0$, so the triangular window $(1 - |\tau|/T)$ is practically equal to $1$. 

What remains is virtually an ideal $\operatorname{sinc}(B\tau)$.

---

### Why doesn't a plain pulse give a sinc?

To see why the chirp is special, consider an **unmodulated rectangular pulse** (constant frequency) of finite length $T$:
* Its autocorrelation is a simple **triangle** of width $2T$.
* Its frequency spectrum is a $\text{sinc}$, but its compressed time response is **not**.

Only when you **chirp** the pulse does time map directly to frequency. The sharp start and end of the chirp in time clip the frequency sweep abruptly at $\pm B/2$, creating the hard rectangular frequency edges that ring out as a $\text{sinc}$ in time.

---

### Practical takeaway in SAR

Because a $\operatorname{sinc}$ has heavy sidelobes (the first sidelobe is only **$-13.3\text{ dB}$** down from the peak, which is why you see those bright rings/sidebands in a $40\text{ dB}$ dynamic range view), operational SAR systems apply a **taper window** (like Hann, Hamming, or Taylor) to the spectrum before compression. This rounds off the hard rectangular edges, trading a slightly wider main peak for dramatically suppressed sidebands.
