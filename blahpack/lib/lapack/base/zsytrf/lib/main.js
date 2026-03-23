

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zsytrf = require( './zsytrf.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zsytrf, 'ndarray', ndarray );


// EXPORTS //

module.exports = zsytrf;
