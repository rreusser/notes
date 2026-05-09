
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zgemlqt = require( './zgemlqt.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zgemlqt, 'ndarray', ndarray );


// EXPORTS //

module.exports = zgemlqt;
