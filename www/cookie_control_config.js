var config = {
    apiKey: '2041dd1ec9de6cf9feabcaef5d4f484bdcdaaa5c',
    product: 'COMMUNITY',
    initialState: "OPEN",
    text : {
        thirdPartyTitle : 'Warning: Some cookies require your attention',
        thirdPartyDescription : 'Consent for some third party cookies can not be automatically revoked. Please follow the link below if you want to opt out of them.'
    },
    optionalCookies: [
        {
            name : 'analytics',
            label: 'Analytical Cookies',
            description: 'Analytical cookies help us to improve our website by collecting and reporting information on its usage.',
            cookies: ['_ga', '_gid', '_gat', '__utma', '__utmt', '__utmb', '__utmc', '__utmz', '__utmv', 'mf_user'],
            recommendedState: true,
            onAccept : function(){
                // Add Google Analytics
                window.dataLayer = window.dataLayer || [];
                function gtag(){dataLayer.push(arguments);}
                gtag('js', new Date());
                gtag('config', 'UA-91956629-1', { 'anonymize_ip':true });
                // End Google Analytics
                
                // Add mouseflow
                window._mfq = window._mfq || [];
                (function() {
                    var mf = document.createElement("script");
                    mf.type = "text/javascript"; mf.async = true;
                    mf.src = "//cdn.mouseflow.com/projects/0b5cc4f0-f12e-4e86-818b-a9b648d9f1a3.js";
                    document.getElementsByTagName("head")[0].appendChild(mf);
                })();
                // End mouseflow
            },
            onRevoke: function(){
                // Disable Google Analytics
                window['ga-disable-UA-91956629-1'] = true;
                // End Google Analytics
                
                // Stop mouseflow recording
                mouseflow && mouseflow.stopSession();
                // End mouseflow
            }
        }
    ]
};

CookieControl.load( config );
