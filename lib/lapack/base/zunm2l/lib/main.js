'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zunm2l = require( './zunm2l.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zunm2l, 'ndarray', ndarray );


// EXPORTS //

module.exports = zunm2l;
