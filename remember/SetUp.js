/* Copyright (C) 2017 Mark D. Blackwell.
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.

See: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Functions
Hide everything from the global namespace, by using an IIFE (Immediately
Invokable Function Expression).
*/

(function() {
    //Keep keyStorage before functions.
    var keyStorage = 'RememberSongs';

    var retrieveQueryParameterComment = function() {
        //queryParameters always includes a leading question mark.
        var queryParameters = window.location.search.slice(1);
        try {
            //IE and Edge lack the URLSearchParams function.
            var queryParametersArray = new URLSearchParams(queryParameters);
            return queryParametersArray.has('comment');
        }
        catch(e) {
            return false;
        }
    }
    var retrieveSongsFromStorage = function() {
        var defaultValue = "[]";
        if (! storageIsAccessible()) {
            return defaultValue;
        }
        var storage = window.localStorage.getItem(keyStorage);
        //window.alert('storage: ' + storage);

        if (null === storage || "" == storage) {
            return defaultValue;
        }
        return storage;
    }
    //See: https://developer.mozilla.org/en-US/docs/Web/API/Web_Storage_API/Using_the_Web_Storage_API
    var storageIsAccessible = function() {
        var hostname;
        var somethingWrongWithHostnameMessage;
        var storage;
        var suggestOperaMessage;

        somethingWrongWithHostnameMessage = 'Something wrong with window.hostname: ';
        suggestOperaMessage = 'Localhost is fine on Opera, but it lacks localStorage in Firefox, etc.';
        try {
            storage = window.localStorage;
            if (null === storage) {
                hostname = window.location.hostname;
                if (null === hostname) {
                    window.alert(somethingWrongWithHostnameMessage + hostname);
                    return false;
                }
                if ('localhost' == hostname) {
                    window.alert(suggestOperaMessage);
                    return false;
                }
                return false;
            }
            var x = '__storage_test__';
            storage.setItem(x, x);
            storage.removeItem(x);
            return true;
        }
        catch(e) {
            //First, do the popup.
            storagePopup(e);

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
        }
    }
    var storagePopup = function(e) {
        window.alert('Remembering songs (across sessions) is disabled:  ' + e);
    }

    var resetSongsDevelopmentOnly = function() {
        var tempSongs = [];
        var tempSongsAsString = JSON.stringify(tempSongs);
        window.localStorage.setItem(keyStorage, tempSongsAsString);
    }
    //resetSongsDevelopmentOnly();

    //TODO: If our usage exceeds localStorage limits, then use IndexedDB, instead.
    //See: https://developer.mozilla.org/en-US/docs/Web/API/IndexedDB_API
    //Retrieve data from localStorage.
    var songsAsString = retrieveSongsFromStorage();
    //window.alert('songsAsString: ' + songsAsString);

    var songsRemembered = JSON.parse(songsAsString);
    
    var showCommentButtons = retrieveQueryParameterComment();
    //window.alert(showCommentButtons);

    var node = document.getElementById('main');
    var app = Elm.Main.embed(node, {
      showCommentButtons: showCommentButtons,
      songsRemembered: songsRemembered
    });

    //Don't use an arrow function ("fat tag"), because IE 11 doesn't support it.
    app.ports.updateLocalStorage.subscribe(function(songsRememberedNew) {
        //window.alert('songsRememberedNew: ' + songsRememberedNew);

        if (storageIsAccessible()) {
            window.localStorage.setItem(keyStorage, JSON.stringify(songsRememberedNew));
        }
    });
})();
