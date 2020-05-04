# general comments

>Could you clarify what 'near-present' means in the Overview section of the README.md?

Is the present date. But normally there is a 45 day lag. We added this at the Overview and paper. "... and ranging from 1981 to near-present (normally with a 45 day lag)"

> Can you add more details for data return performance and what the user should expect for big data calls in the README? Is the package most useful for a few point data returns or can the user get a continental data call returned in a reasonable amount of time? I see some description of limitations in the Overview vignette, but I think the best uses for the chirps dataset, package, and any limitations should be upfront so a potential user can decide to install the package before finding a limitation description in the vignette. I could not run the example code successfully. I let the example code in the README run for about 20 min before it timed out and returned an error.

The example in the README should go fast (~ 45 s), but the server was experiencing some issues. In any case, if the number of points is scaled (e.g. globaly) this may increase the time for data retrieval significantly. We added this message in the pkg DESCRIPTION. "Requests from large time series (> 10 years) and large geographic coverage (global scale) may take several minutes."

>Is there a recommended timeout for the data request? Although get_chirps() worked earlier this morning, it seems like the server is down again, and I get the following output with the paper's example script:

By the time of your review the developers of ClimaSERV were working in some improvements in the server. The requests should work fine now, but with the limitations stated in the DESCRIPTION file.

>It's pretty clear that the server is down, but if a user does not know that the server is down, would you expect multiple 0%... data progress from big data calls? Would this data call timeout if I left it going when the server is down? A little more guidance on the call request time would help the user debug in these instances.

This message was removed from the .GET() function. Now the function tries to get the data 6 times (six tries), if no data is returned than an error message is provided saying that the server is down.