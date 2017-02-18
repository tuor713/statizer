Vue.component('status-table',{
    props:['signals'],
    template:'#component-status-table'
})

Vue.component('signal',{
    props:['signal'],
    template:'#component-signal'
})

const SignalView = {
    template: '<div v-if="loaded"><signal v-bind:signal="signal"></signal></div>',
    data: function () {
        var self = this;
        $.get('/api/signal/' + this.$route.params.id + '/full', null, function (data) {
            self.signal = makeSignal(data);
            self.loaded = true;
        });

        return { signal:null, loaded:false };
    },
    watch: {
        '$route' (to, from) {
            var self = this;
            $.get('/api/signal/' + this.$route.params.id + '/full', null, function (data) {
                self.signal = makeSignal(data);
                self.loaded = true;
            });
        }
    }
};

const HomeView = {
    template: '<div>\
<status-table v-bind:signals=signals></status-table>\
<button class="btn btn-primary" v-on:click="refresh">Refresh</button>\
</div>',
    methods: {
        refresh: function () {
            var self = this;
            $.get('/api/signal/all', null, function (data) {
                self.signals = data.map(function (it) { return makeSignal(it); });
            });
        }
    },
    data: function () {
        var self = this;
        $.get('/api/signal/all', null, function (data) {
            self.signals = data.map(function (it) { return makeSignal(it); });
        });
        return { signals:[] };
    }
};


const routes = [
    { path: '/signal/:id', component: SignalView },
    { path: '/', component: HomeView }
];

const router = new VueRouter({
    routes: routes
});

const app = new Vue({
    router: router
}).$mount('#app');

function makeSignal(data) {
    data.isError = function () { return this.value != null && this.value <= 0.5; };
    data.isWarning = function () { return this.value == null || (this.value < 1 && this.value > 0.5); };
    data.isSuccess = function () { return this.value == 1; };
    data.formattedValue = function () {
        if (this.value == null)
            return 'Undefined';
        else
            return Math.round(100*this.value);
    };

    data.hasDependencies = function () { return this.dependencies != null && this.dependencies.length != 0; };
    if (data.dependencies != null) {
        data.dependencies = data.dependencies.map(function (it) { return makeSignal(it); });
    }

    return data;
}
