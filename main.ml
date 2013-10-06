let main () =
        Sdl.init [`VIDEO];
        Sdlvideo.set_video_mode 300 300 [`DOUBLEBUF];
        Sdltimer.delay 3000;
        exit 0


let () = main ()
