
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlansb = require( './zlansb.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlansb, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlansb;
