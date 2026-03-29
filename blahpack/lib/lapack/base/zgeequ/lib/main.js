'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zgeequ = require( './zgeequ.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zgeequ, 'ndarray', ndarray );


// EXPORTS //

module.exports = zgeequ;
