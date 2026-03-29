'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zsymm = require( './zsymm.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zsymm, 'ndarray', ndarray );


// EXPORTS //

module.exports = zsymm;
