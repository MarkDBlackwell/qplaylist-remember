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
    var saveSongs = function(songsRememberedNew) {
//window.alert('songsRememberedNew: ' + songsRememberedNew);
        window.localStorage.setItem(keyStorage, JSON.stringify(songsRememberedNew));
    }
    //See: https://developer.mozilla.org/en-US/docs/Web/API/Web_Storage_API/Using_the_Web_Storage_API
    var storageIsAvailable = function() {
        try {
            storageTry();
            return true;
        }
        catch(e) {
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
                storage.length !== 0;
        }
    }
    var storagePopupMaybe = function() {
        try {
            storageTry();
        }
        catch(e) {
            window.alert('Remembering songs (across sessions) is disabled:  ' + e);
        }
    }
    var storageTry = function() {
        var storage = window['localStorage'],
            x = '__storage_test__';
        storage.setItem(x, x);
        storage.removeItem(x);
    }

    storagePopupMaybe();
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

    app.ports.updateLocalStorage.subscribe(songsRememberedNew => {
        saveSongs(songsRememberedNew);
    });
})();