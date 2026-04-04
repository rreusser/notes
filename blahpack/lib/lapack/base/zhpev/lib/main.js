
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zhpev = require( './zhpev.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zhpev, 'ndarray', ndarray );


// EXPORTS //

module.exports = zhpev;
