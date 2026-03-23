

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zlauu2 = require( './zlauu2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlauu2, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlauu2;
