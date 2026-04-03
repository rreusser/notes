
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zsptrf = require( './zsptrf.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zsptrf, 'ndarray', ndarray );


// EXPORTS //

module.exports = zsptrf;
