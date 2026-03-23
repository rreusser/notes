

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zgebd2 = require( './zgebd2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zgebd2, 'ndarray', ndarray );


// EXPORTS //

module.exports = zgebd2;
