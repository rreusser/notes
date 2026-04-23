

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zupgtr = require( './zupgtr.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zupgtr, 'ndarray', ndarray );


// EXPORTS //

module.exports = zupgtr;
