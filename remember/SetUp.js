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
*/

//See: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Functions
//Hide everything from the global namespace, by using an IIFE (Immediately
//    Invokable Function Expression).

(function() {

    var keyStorage = 'RememberSongs';

    var resetSongsDevelopmentOnly = function() {
        var tempSongs = [];
        var tempSongsAsString = JSON.stringify(tempSongs);
        window.localStorage.setItem(keyStorage, tempSongsAsString);
    }
    var retrieveQueryParameterComment = function() {
        //Always includes the leading question mark.
        var queryParameters = window.location.search.slice(1);
        //IE and Edge lack the URLSearchParams function.
        try {
            var queryParametersArray = new URLSearchParams(queryParameters);
            return queryParametersArray.has('comment');
        }
        catch(e) {
            return false;
        }
    }
    var retrieveSongsFromStorage = function() {
        var defaultValue = "[]";
        if (! storageIsAvailable()) {
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
    var storageIsAvailable = function() {
        try {
            var storage = window.localStorage;
            var x = '__storage_test__';
            storage.setItem(x, x);
            storage.removeItem(x);
            return true;
        }
        catch(e) {
            storagePopup(e);
            return e instanceof DOMException && (
                // everything except Firefox
                e.code === 22 ||
                // Firefox
                e.code === 1014 ||
                // test name field too, because code might not be present
                // everything except Firefox
                e.name === 'QuotaExceededError' ||
                // Firefox
                e.name === 'NS_ERROR_DOM_QUOTA_REACHED') &&
                // acknowledge QuotaExceededError only if there's something already stored
                storageMy.length !== 0;
        }
    }
    var storagePopup = function(e) {
        window.alert('Remembering songs (across sessions) is disabled:  ' + e);
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

    //Don't use arrow function (fat tag), because IE 11 doesn't support it.
    app.ports.updateLocalStorage.subscribe(function(songsRememberedNew) {
//window.alert('songsRememberedNew: ' + songsRememberedNew);
        if (storageIsAvailable()) {
            window.localStorage.setItem(keyStorage, JSON.stringify(songsRememberedNew));
        }
    });
})();
