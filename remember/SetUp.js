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

/*
var functionResetSongsDevelopmentOnly = function() {
    var tempSongs = [];
    var tempSongsAsString = JSON.stringify(tempSongs);
    var keyStorage = 'RememberSongs';
    window.localStorage.setItem(keyStorage, tempSongsAsString);
}
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

        var app;

        functionAttachNode = function() {
            var node;

            node = document.getElementById('main');
            app = Elm.Main.embed(node, {
                showCommentButtons: functionShowCommentButtons(),
                songsRemembered: functionSongsRememberedRetrieved()
            });
        }
        functionKeyStorage = function() {
            return 'RememberSongs';
        }
        functionShowCommentButtons = function() {
            var queryParameters;

            //location.search always includes a leading question mark.
            queryParameters = window.location.search.slice(1);

            //IE and Edge lack the URLSearchParams function, so don't use it.
            return 'comment' == queryParameters;
        }
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
                }
                storage = window.localStorage.getItem(functionKeyStorage());
                //window.alert('storage: ' + storage);

                if (null === storage || "" == storage) {
                    return defaultValue;
                }
                return storage;
            }
            return JSON.parse(functionRetrieveSongsFromStorageAsString());
        }
        functionStorageIsAccessible = function() {
            //See: https://developer.mozilla.org/en-US/docs/Web/API/Web_Storage_API/Using_the_Web_Storage_API
            var hostname;
            var somethingWrongWithHostnameMessage;
            var storage;
            var suggestOperaMessage;
            var x;

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
                x = '__storage_test__';
                storage.setItem(x, x);
                storage.removeItem(x);
                return true;
            }
            catch(e) {
                //First, do the popup.
                window.alert('Remembering songs (across sessions) is disabled:  ' + e);

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
        functionStorageSubscribe = function() {
            //Don't use an arrow function ("fat tag"), because IE 11 doesn't support it.
            app.ports.updateLocalStorage.subscribe(function(songsRememberedFromPort) {
                if (functionStorageIsAccessible()) {
                    window.localStorage.setItem(functionKeyStorage(), JSON.stringify(songsRememberedFromPort));
                }
            });
        }

        functionAttachNode();
        functionStorageSubscribe();
    }

    functionDealWithElm();
})();
