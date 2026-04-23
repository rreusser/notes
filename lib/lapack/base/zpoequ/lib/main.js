'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zpoequ = require( './zpoequ.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zpoequ, 'ndarray', ndarray );


// EXPORTS //

module.exports = zpoequ;
