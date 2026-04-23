
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlantb = require( './zlantb.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlantb, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlantb;
