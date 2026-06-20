# VHS tape for the DCF-SDR demo GIF. Reproducible (text, not a binary blob):
#   render with:  nix run nixpkgs#vhs -- Documentation/media/dcf-sdr-demo.tape
#   (run from the repo root, in an env where `python3` has numpy — e.g. `nix develop .#sdr`).
# Produces Documentation/media/dcf-sdr-demo.gif, embedded in the README "Over the air" section.
Output Documentation/media/dcf-sdr-demo.gif

Set Shell bash
Set FontSize 17
Set Width 1180
Set Height 720
Set Padding 18
Set Theme "Dracula"
Set TypingSpeed 28ms

Hide
Type "clear" Enter
Show

Type "# DCF over the air: the same 17-byte frame, now on radio (no internet)" Enter
Sleep 900ms
Type "#   frame -> RS-FEC -> GFSK IQ -> .cf32 file -> recovered frame" Enter
Sleep 900ms
Type "bash python/modem/demo.sh" Enter
Sleep 6s
Sleep 2500ms
