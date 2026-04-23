
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zgetc2 = require( './zgetc2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zgetc2, 'ndarray', ndarray );


// EXPORTS //

module.exports = zgetc2;
