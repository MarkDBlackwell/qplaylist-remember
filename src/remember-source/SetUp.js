/* Copyright (C) 2017 Mark D. Blackwell.
   All rights reserved.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*/

/* See:
https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Functions

Hide everything from the global namespace, by using an IIFE (Immediately
Invokable Function Expression).
*/

/*
var functionResetSongsDevelopmentOnly = function() {
    var keyStorage;
    var localStorage;
    var tempSongs;
    var tempSongsAsString;

    tempSongs = [
        {
          artist: "Vance Joy",
          likeOrCommentCount: 1,
          title: "Lay It On Me",
          time: "7:26 AM",
          timestamp: "2017 08 31 07 26"
        },
        {
          artist: "The Family Crest",
          likeOrCommentCount: 0,
          title: "Mirror Love",
          time: "7:22 AM",
          timestamp: "2017 08 31 07 22"
        },
      ];
    tempSongsAsString = JSON.stringify(tempSongs);
    keyStorage = 'RememberSongs';
    localStorage = window.localStorage;
    window.alert('localStorage: ' + localStorage);
    if (null === localStorage) {
        window.alert('localStorage is null.');
    } else {
        try {
            localStorage.setItem(keyStorage, tempSongsAsString);
        };
        catch(e) {
            window.alert('e: ' + e);
        };
    };
    return null;
};
functionResetSongsDevelopmentOnly();
*/

(function() {
    var functionDealWithElm;

    functionDealWithElm = function() {
        var functionAttachNode;
        var functionKeyStorage;
        var functionShowCommentButtons;
        var functionSongsRememberedRetrieved;
        var functionStorageIsAccessible;
        var functionStorageSubscribe;

        functionAttachNode = function() {
            var node;

            node = document.getElementById('main');
            return Elm.Main.embed(node, {
                showCommentButtons: functionShowCommentButtons(),
                songsRemembered: functionSongsRememberedRetrieved()
            });
        };
        functionKeyStorage = function() {
            return 'RememberSongs';
        };
        functionShowCommentButtons = function() {
            var queryParameters;

            //location.search always includes a leading question mark.
            queryParameters = window.location.search.slice(1);

            //IE and Edge lack the URLSearchParams function, so don't use it.
            return 'comment' == queryParameters;
        };
        functionSongsRememberedRetrieved = function() {
            var functionRetrieveSongsFromStorageAsString;

            functionRetrieveSongsFromStorageAsString = function() {
                //TODO: If our usage exceeds localStorage limits, then use IndexedDB, instead.
                //See: https://developer.mozilla.org/en-US/docs/Web/API/IndexedDB_API
                //Retrieve data from localStorage.

                var defaultValue;
                var storage;

                defaultValue = "[]";
                if (! functionStorageIsAccessible()) {
                    return defaultValue;
                };
                storage = window.localStorage.getItem(functionKeyStorage());
                //window.alert('storage: ' + storage);

                if (null === storage || "" == storage) {
                    return defaultValue;
                };
                return storage;
            };
            return JSON.parse(functionRetrieveSongsFromStorageAsString());
        };
        functionStorageIsAccessible = function() {
            var rememberingSongsIsDisabledMessage;
            var somethingWrongWithLocalStorageMessage;
            var storage;
            var x;


            //See: https://developer.mozilla.org/en-US/docs/Web/API/Web_Storage_API/Using_the_Web_Storage_API
            try {
                storage = window.localStorage;
                if (null === storage) {
                    somethingWrongWithLocalStorageMessage = 'Something wrong with window.localStorage';
                    window.alert(somethingWrongWithLocalStorageMessage + ': ' + storage);
                    return false;
                }
                x = '__storage_test__';
                storage.setItem(x, x);
                storage.removeItem(x);
                return true;
            }
            catch(e) {
                //First, do the popup.
                rememberingSongsIsDisabledMessage = 'Remembering songs (across sessions) is disabled';
                window.alert(rememberingSongsIsDisabledMessage + ':  ' + e);

                //Avoid QuotaExceededError, but only if the exception is a DOMException...
                return e instanceof DOMException &&

                    //and storage exists...
                    null !== storage &&

                    //and storage isn't undefined...
                    undefined !== storage &&

                    //and there's something already stored...
                    storage.length !== 0 && (

                        //and the exception is one of certain, known ones:
                        //Because the name field might not be set, first check the error code...
                        //for Firefox:
                        //Legacy constant name: NS_ERROR_DOM_QUOTA_REACHED.
                        e.code === 1014 ||

                        //for everything except Firefox:
                        //Legacy constant name: QUOTA_EXCEEDED_ERR.
                        e.code === 22 ||

                        //Because the error code might not be set, also check the name field...
                        //for Firefox:
                        e.name === 'NS_ERROR_DOM_QUOTA_REACHED' ||

                        //for everything except Firefox:
                        e.name === 'QuotaExceededError'
                    );
            };
        };
        functionStorageSubscribe = function(app) {
            //Don't use an arrow function ("fat tag"), because IE 11 doesn't support it.
            app.ports.updateLocalStorage.subscribe(function(songsRememberedFromPort) {
                var stringified;
                if (functionStorageIsAccessible()) {
                    stringified = JSON.stringify(songsRememberedFromPort);
                    window.localStorage.setItem(functionKeyStorage(), stringified);
                };
            });
            return null;
        };

        return functionStorageSubscribe(functionAttachNode());
    };

    return functionDealWithElm();
})();
